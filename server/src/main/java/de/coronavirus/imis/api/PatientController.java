package de.coronavirus.imis.api;

import de.coronavirus.imis.api.dto.CreatePatientDTO;
import de.coronavirus.imis.api.dto.PatientSearchParamsDTO;
import de.coronavirus.imis.api.dto.PatientSimpleSearchParamsDTO;
import de.coronavirus.imis.domain.Patient;
import de.coronavirus.imis.services.PatientService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/patients")
@RequiredArgsConstructor
public class PatientController {

    private final PatientService patientService;

    @PostMapping
    public ResponseEntity<Patient> addPatient(@RequestBody CreatePatientDTO dto) {
        var patient = patientService.addPatient(dto);
        if (patient == null) {
            return ResponseEntity.status(500).build();
        }
        return ResponseEntity.ok(patient);
    }

    @GetMapping("/{id}")
    public ResponseEntity<Patient> getPatientForId(@PathVariable("id") String id) {
        return patientService.findPatientById(id).map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }

    @GetMapping
    public ResponseEntity<List<Patient>> getAllPatients() {
        return ResponseEntity.ok(patientService.getAllPatients());
    }


    @PostMapping("/query-simple")
    public List<Patient> queryPatientsSimple(@RequestBody PatientSimpleSearchParamsDTO query) {
        return patientService.queryPatientsSimple(query);
    }

    @GetMapping("/query-simple/count")
    public Long countQueryPatientsSimple(@RequestParam String query) {
        return patientService.queryPatientsSimpleCount(query);
    }

    @PostMapping("/query")
    public List<Patient> queryPatients(@RequestBody final PatientSearchParamsDTO patientSearchParamsDTO) {
        return patientService.queryPatients(patientSearchParamsDTO);
    }

    @PostMapping("/query/count")
    public Long countQueryPatients(@RequestBody final PatientSearchParamsDTO patientSearchParamsDTO) {
        return patientService.countQueryPatients(patientSearchParamsDTO);
    }
}
